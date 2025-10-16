import sys
from datetime import datetime
from isoduration import parse_duration
from isoduration.types import Duration


def add_date() -> None:

    date: datetime = datetime.fromisoformat(sys.argv[1])
    duration: Duration = parse_duration(sys.argv[2])
    print((date + duration).isoformat())


