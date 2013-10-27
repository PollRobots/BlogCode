#!/usr/bin/python

import argparse
import json
import re

from datetime import datetime
from os.path import expanduser
from urllib import urlencode
from urllib2 import urlopen

CONFIG_FILES = ['~/.weatherrc', '~/_weatherrc']

LAYERS = {
        'FOREGROUND':3,
        'BACKGROUND':4,
        }

COLORS = {
        'BLACK':0,
        'RED':1,
        'GREEN':2,
        'YELLOW':3,
        'BLUE':4,
        'MAGENTA':5,
        'CYAN':6,
        'WHITE':7,
    }

EFFECTS = {
        'RESET':0,
        'BOLD':1,
        'UNDERLINE':4,
        'BLINK':5,
        'REVERSE':7,
        'HIDE':8,
        'NOBOLD':22,
        'NOUNDERLINE':24,
        'NOBLINK':25,
        'NOREVERSE':27,
        'SHOW':28,
        }

def _ansi_code(*args):
    layer = 3 # by default assume a foreground color is being set.
    actions = []
    for a in args:
        key = a.upper()
        if key in LAYERS:
            layer = LAYERS[key]
        elif key in COLORS:
            actions.append(layer * 10 + COLORS[key])
        elif key in EFFECTS:
            actions.append(EFFECTS[key])

    return u'\033[%sm' % (';'.join(str(a) for a in actions),)

def _strip_ansi(string):
    return ''.join(re.split('\033\\[[^m]*m', string))

_config = {}

def _read_config():
    config = {}
    for cf in CONFIG_FILES:
        try:
            with open(expanduser(cf)) as c:
                for line in c.readlines():
                    line = line.strip()
                    if not line or line.startswith('#'):
                        continue

                    elements = line.split(':', 1)
                    if elements and len(elements) == 2:
                        config[elements[0]] = elements[1]
            return config
        except IOError:
            pass

    return {}

def _get_config(name, default = None):
    if not _config:
        _config.update(_read_config())

    return _config.get(name, default)

_print_symbols = dict(
        background=_get_config('background', _ansi_code('background', 'blue')),
        text=_get_config('text', _ansi_code('cyan', 'bold')),
        data=_get_config('data', _ansi_code('yellow', 'bold')),
        delimiter=_get_config('delimiter', _ansi_code('magenta') + u'=>'),
        dashes=_get_config('dashes', _ansi_code('blue') + u'-'),
        reset = _ansi_code('reset'),
        )

_title_row = _get_config('title-row', _ansi_code('background', 'black', 'foreground', 'blue', 'bold'))
_even_row = _get_config('even-row', _ansi_code('background', 'white', 'foreground', 'black'))
_odd_row = _get_config('even-row', _ansi_code('background', 'cyan', 'foreground', 'black'))

_sun = _get_config('sun', _ansi_code('yellow', 'bold') + u'\u2600')
_moon = _get_config('moon', _ansi_code('cyan') + u'\u263d')
_clouds = _get_config('clouds', _ansi_code('white', 'bold') + u'\u2601')
_rain = _get_config('rain', u'\u2614')
_snow = _get_config('snow', _ansi_code('white', 'bold') + u'\u2744')

def _print_table(cols, data):
    """Print tabulated data.

    cols - This is a list of 3-tuples, of (title, name, suffix)
    data - This is a list of dicts
    """
    #compute widths

    # initialize to titles
    widths = [len(t) for (t, _, _) in cols]

    for row in data:
        for i, (_, c, s) in enumerate(cols):
            w = len(_strip_ansi(unicode(row.get(c, '')))) + len(s)
            if w > widths[i]:
                widths[i] = w

    # print title row
    line = _title_row
    for i, (t, _, _) in enumerate(cols):
        line += t.title() + (' ' * (widths[i] - len(t)))
    line += _ansi_code('reset')
    print line

    # print table rows
    for n, row in enumerate(data):
        row_code = _odd_row if n % 2 else _even_row
        line = row_code
        for i, (_, c, s) in enumerate(cols):
            item = unicode(row.get(c, '')) + unicode(s)
            line += item + (' ' * (widths[i] - len(_strip_ansi(item))))
            if '\033' in item:
                line += _ansi_code('reset') + row_code
        line += _ansi_code('reset')
        print line


class Weather(object):
    'Encapsulates simple access to the openweathermap.org API'

    BASE_URI = 'http://api.openweathermap.org/data/2.5'
    WEATHER = 'weather'
    FIND_CITY = 'find'
    FORECAST = 'forecast'
    DAILY = 'forecast/daily'

    DISPLAY_FORMAT = u"""
%(background)s%(text)s Current weather in %(city)s 
%(delimiter)s%(data)s %(temperature)s%(scale)s %(icon)s 
%(dashes)s%(text)s Humidity %(delimiter)s%(data)s %(humidity)s%% 
%(dashes)s%(text)s Pressure %(delimiter)s%(data)s %(pressure)s hPa %(reset)s
""".replace('\n','')

    def __init__(self, weather, units = None, city = None):
        if not weather:
            return
        main = weather.get('main')
        if main is None:
            main = weather

        self.scale = u'\u02daC' if (units or _get_config('units', 'metric')) == 'metric' else u'\u02daF'

        self.city = city or weather.get('name')
        self.temperature = main.get('temp')
        if isinstance(self.temperature, dict):
            self.min = int(round(self.temperature.get('min')))
            self.max= int(round(self.temperature.get('max')))
            self.temperature = '%s%s %s' % (int(round(self.temperature.get('max'))), 
                                            self.scale,
                                            int(round(self.temperature.get('min'))))
        else:
            self.temperature = int(round(self.temperature))

        self.humidity = int(round(main.get('humidity')))
        self.pressure = int(round(main.get('pressure')))

        self.sky = weather.get('weather')[0].get('main')

        sys = weather.get('sys')
        dt = weather.get('dt')
        self.time = datetime.utcfromtimestamp(dt)
        self.local_time = datetime.fromtimestamp(dt)
        if sys and sys.get('sunrise'):
            self.sunrise = datetime.utcfromtimestamp(sys.get('sunrise'))
            self.sunset = datetime.utcfromtimestamp(sys.get('sunset'))

            self.is_night = self.time >= self.sunset or self.time <= self.sunrise
        else:
            self.is_night = False

        self.scale = u'\u02daC' if (units or _get_config('units', 'metric')) == 'metric' else u'\u02daF'

        if self.sky == 'Clear':
            icon = _moon if self.is_night else _sun
        elif self.sky == 'Clouds':
            icon = _clouds
        elif self.sky == 'Rain':
            icon = _rain
        elif self.sky == 'Snow':
            icon = _snow
        else:
            icon = self.sky

        self.icon = icon

    def display(self, symbols=None, uses_ansi=True, show_time=False):
        """Displays the instance as a string using ANSI color codes.

        If symbols is supplied and true, or if the default is true, then unicode symbols are used
        for common weather conditions."""
        if symbols is None:
            symbols = _get_config('symbols', 'true') == 'true'

        args = _print_symbols.copy()
        args.update(dict(
            city=self.city,
            temperature=self.temperature,
            humidity=int(round(self.humidity)),
            pressure=int(round(self.pressure)),
            icon=self.icon if symbols else self.sky,
            scale=self.scale,
            time=self.local_time,
            ))

        output = Weather.DISPLAY_FORMAT % args
        if not uses_ansi:
            output = _strip_ansi(output)

        return output

    @classmethod
    def get_weather(cls, location = None, units = None, app_id = None):
        """Gets the weather for the specified location.

        The weather will be reported in either metric or imperial units.
        Default values for parameters that are not supplied will be read from a configuration file.

        This returns a Weather object on success or an error string on failure"""

        city = location or _get_config('location')
        units = units or _get_config('units')
        response = cls._make_request(cls.WEATHER, app_id, q=city, units=units)

        if not response.get('main'):
            return response.get('message', 'Unknown error')

        return Weather(response, units, city)

    @classmethod
    def get_forecast(cls, location=None, units=None, daily=False, app_id=None):
        """Gets a forecast for the specified location.

        This returns a list of Weather objects on success; or an error string otherwise"""

        city = location or _get_config('location')
        units = units or _get_config('units')
        response = cls._make_request(cls.DAILY if daily else cls.FORECAST, app_id, q=city, units=units)

        forecasts = response.get('list')
        if not forecasts:
            return response.get('message', 'Unknown error')

        city = response.get('city', {}).get('name', city)

        return map(lambda x: Weather(x, units, city), forecasts)

    @classmethod
    def find_city(cls, location, app_id = None):
        """Finds cities in the openweathermap.org database.

        If no matching cities are found, returns an empty list, otherwise returns a list of
        (city, country) tuples."""

        response = cls._make_request(cls.FIND_CITY, app_id, q=location)

        cities = []
        for city in response.get('list', []):
            n = city.get('name')
            c = city.get('sys').get('country')
            cities.append((n, c))

        return set(cities)

    @classmethod
    def _make_request(cls, route, app_id, **kwargs):
        appid = app_id or _get_config('app-id', None)

        query = kwargs.copy()
        if appid:
            query['APPID'] = appid

        url = '%s/%s?%s' % (cls.BASE_URI, route, urlencode(query))
        response = urlopen(url)
        content = response.read()
        return json.loads(content)

def _make_parser():
    dsym = 'symbols' if _get_config('symbols', 'true') == 'true' else 'text'

    parser = argparse.ArgumentParser(description='Display weather information')
    parser.add_argument('location', nargs='?', 
            help='The location for which weather should be fetched. Defaults to: %s' % _get_config('location'))
    parser.add_argument('-u', '--units', nargs='?', choices=['metric', 'imperial'], 
            help='The units to use when displaying the temperature. Defaults to: %s' % _get_config('units'))
    parser.add_argument('-d', '--display', nargs='?', choices=['symbols', 'text'], default=dsym,
            help='Display weather condition using a symbol or text. Defaults to: %s' % dsym)
    parser.add_argument('-f', '--forecast', action='count', default=0, help='Fetch a weather forecast')
    parser.add_argument('-l', '--lookup', action='count', default=0, help='Lookup city name')
    parser.add_argument('-df', '--daily', action='count', default=0, help='Fetch a daily weather forecast')
    return parser

if __name__ == '__main__':
    p = _make_parser()
    a = p.parse_args()

    if a.lookup > 0:
        cities = Weather.find_city(a.location)
        if not cities:
            print "No matching cities were found"
        else:
            for n, c in cities:
                print "%s, %s" % (n, c)
    else:
        w = Weather.get_weather(a.location, a.units)
        if hasattr(w, 'display'):
            use_symbols = a.display == 'symbols'
            print w.display(use_symbols)
            if a.forecast + a.daily > 0:
                fs = Weather.get_forecast(a.location, a.units, daily=a.daily > 0)
                if isinstance(fs, list):
                    scale = fs[0].scale
                    space = (' ', ' ','')
                    if a.daily > 0:
                        cols = [('max', 'max', scale), space,
                                ('min', 'min', scale), space,
                                ('', 'icon', ' ') if use_symbols else ('', 'sky', ''), space,
                                ('humidity', 'humidity', '%'), space,
                                ('pressure', 'pressure', ' hPa'), space]
                    else:
                        cols = [(' ', 'time', ''), space,
                                ('temp', 'temperature', scale), space,
                                ('', 'icon', ' ') if use_symbols else ('', 'sky', ''), space,
                                ('humidity', 'humidity', '%'), space,
                                ('pressure', 'pressure', ' hPa'), space,]


                    _print_table(cols, map(lambda x: x.__dict__, fs))
                else:
                    print f
        else:
            print w

