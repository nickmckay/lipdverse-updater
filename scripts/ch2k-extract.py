#!/usr/bin/env python3
"""Extract the CoralHydro2k MATLAB structure into JSON for the R converter.

    ./scripts/ch2k-extract.py ~/Downloads/hydro2kv2_0_3.mat out.json

The .mat is MATLAB v7.3, which is HDF5, so R cannot read it without extra
packages; Python can, and the conversion proper belongs in R where lipdR
writes the files. This does nothing but transcribe -- no mapping, no renaming,
no judgement. That all happens in ch2k-to-lipd.R against a reviewable table.

Two shapes worth knowing:

  * `ch2k` holds one struct per core AND a top-level `version` char. Counting
    members as cores overstates by one (227 cores, not 228).
  * A data field is a 2 x N array: row 0 is year, row 1 is the value. That is
    MATLAB's N x 2 seen through HDF5's transpose.
"""
import json, sys
import h5py
import numpy as np

# The measurement variables, as convert.m names them. The `_err` fields are
# uncertainty VECTORS the same length as their parent, not scalars; they become
# their own columns, paired to the parent's year axis.
VARIABLES = ["d18O", "SrCa", "d18O_sw",
             "d18O_annual", "SrCa_annual", "d18O_sw_annual"]
UNCERTAINTY = {"d18O_err": "d18O", "SrCa_err": "SrCa"}


def _val(f, obj):
    if isinstance(obj, h5py.Reference):
        return _val(f, f[obj])
    if isinstance(obj, h5py.Group):
        return {k: _val(f, obj[k]) for k in obj}
    a = obj[()]
    if obj.attrs.get("MATLAB_empty", 0) == 1:
        return None
    if isinstance(a, np.ndarray) and a.dtype == object:
        vals = [_val(f, r) for r in a.ravel()]
        return vals[0] if len(vals) == 1 else vals
    if obj.attrs.get("MATLAB_class", b"").decode() == "char":
        s = "".join(chr(int(c)) for c in np.atleast_2d(a).ravel(order="F"))
        return s.strip() or None
    if isinstance(a, np.ndarray):
        a = np.squeeze(a)
        if a.size == 0:
            return None
        if a.size == 1:
            return float(a.ravel()[0])
        return a
    return a


def main(src, dst):
    f = h5py.File(src, "r")
    root = f["ch2k"]
    out = {"version": None, "cores": {}}
    for name in root:
        obj = root[name]
        if not isinstance(obj, h5py.Group):
            if name == "version":
                out["version"] = _val(f, obj)
            continue
        rec = {"meta": {}, "data": {}}
        for fld in obj:
            v = _val(f, obj[fld])
            if v is None:
                continue
            if isinstance(v, np.ndarray) and v.ndim == 2 and v.shape[0] == 2:
                # year / value pair
                year, value = v[0], v[1]
                rec["data"][fld] = {
                    "year": [None if np.isnan(x) else float(x) for x in year],
                    "value": [None if np.isnan(x) else float(x) for x in value],
                }
            elif isinstance(v, np.ndarray):
                # A bare vector: the uncertainty fields arrive this way, with no
                # year of their own. They are paired downstream.
                rec["data"][fld] = {
                    "year": None,
                    "value": [None if np.isnan(x) else float(x) for x in np.ravel(v)],
                }
            else:
                rec["meta"][fld] = v
        out["cores"][name] = rec

    with open(dst, "w") as fh:
        json.dump(out, fh)

    n_data = sum(len(c["data"]) for c in out["cores"].values())
    print(f"version      : {out['version']}")
    print(f"cores        : {len(out['cores'])}")
    print(f"data columns : {n_data}")
    have = {}
    for c in out["cores"].values():
        for k in c["data"]:
            have[k] = have.get(k, 0) + 1
    for k in sorted(have, key=lambda x: -have[x]):
        note = ""
        if k in UNCERTAINTY:
            note = f"  (uncertainty for {UNCERTAINTY[k]})"
        elif k not in VARIABLES:
            note = "  (not a measurement variable)"
        print(f"   {k:22s} {have[k]:4d} cores{note}")
    print(f"\nwrote {dst}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
