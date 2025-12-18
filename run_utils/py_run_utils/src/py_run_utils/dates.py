import sys
from datetime import datetime
from isoduration import parse_duration
import re

def date_add() -> None:
    date = datetime.fromisoformat(sys.argv[1][:-1])
    duration = parse_duration(sys.argv[2])
    print((date + duration).isoformat() + "Z")

def date_min() -> None:
    date_1 = datetime.fromisoformat(sys.argv[1][:-1])
    date_2 = datetime.fromisoformat(sys.argv[2][:-1])
    print(max(date_1, date_2).isoformat() + "Z")

def date_cycle() -> None:
    current_date = datetime.fromisoformat(sys.argv[1][:-1])
    duration = parse_duration(sys.argv[2])
    end_date = datetime.fromisoformat(sys.argv[3][:-1])
    print(min(current_date + duration, end_date).isoformat() + "Z")

def duration_larger(d1_str: str, d2_str: str) -> bool:
    dummy_reference_date = datetime(2000,1,1)
    d1 = parse_duration(d1_str)
    d2 = parse_duration(d2_str)
    return dummy_reference_date + d1 > dummy_reference_date + d2

def check_file_interval() -> None:
    for nml_path in sys.argv[1:-1]:
        check_file_interval_nml(nml_path, sys.argv[-1])

def check_file_interval_nml(nml_path: str, restart_interval: str) -> None:
    interval_pattern = re.compile(r"^\s*file_interval\s*=\s*\"*([0-9a-zA-Z]*)\"*\s*.*$")
    with open(nml_path) as nml:
        for line in nml:
            if (interval_match := interval_pattern.match(line)) is not None:
                file_interval = interval_match.group(1)
                if duration_larger(file_interval, restart_interval):
                    raise ValueError(f"in {nml_path}: found file interval {file_interval} larger than restart interval {restart_interval}")
