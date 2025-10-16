import sys
from datetime import datetime
from isoduration import parse_duration


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

