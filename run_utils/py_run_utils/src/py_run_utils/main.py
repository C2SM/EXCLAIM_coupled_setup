import sys
from datetime import datetime
from isoduration import parse_duration
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from isoduration.types import Duration


def date_add() -> None:

    date: datetime = datetime.fromisoformat(sys.argv[1][:-1])
    duration: Duration = parse_duration(sys.argv[2])
    print((date + duration).isoformat() + "Z")


