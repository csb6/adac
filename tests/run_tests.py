import subprocess, sys
from pathlib import Path

root_dir = Path("./third_party/gnat-walnut-creek-Mar-94/ver_167/sparc/gnat-1.67-src/src")
# These are hierarchical units (not supported in Ada83)
ignored_units = {"sinfo-change", "system-traceback"}
unit_names = {f.stem for f in root_dir.iterdir() if f.suffix in [".adb"]} - ignored_units
unit_names = sorted(unit_names)
for unit_name in unit_names:
    print(unit_name)
    status = subprocess.run(["build/adac", "-I", str(root_dir), unit_name], stderr=subprocess.STDOUT)
    if status.returncode != 0:
        print("Returned error code:", status.returncode)
        break
