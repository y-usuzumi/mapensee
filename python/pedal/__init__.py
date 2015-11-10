from . import syntax as _pedal  # Some naughty people might use _pedal
from .transcript import transcript

parse = _pedal.parse

__all__ = ['parse', 'transcript']
