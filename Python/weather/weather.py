#!/usr/bin/python

import argparse
import json

from datetime import datetime
from os.path import expanduser
from urllib import urlencode
from urllib2 import urlopen

CONFIG_FILES = ['~/.weatherrc', '~/_weatherrc']

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
        background=_get_config('background', u'\033[44m'),
        text=_get_config('text', u'\033[36;1m'),
        data=_get_config('data', u'\033[33;1m'),
        delimiter=_get_config('delimiter', u'\033[35m=>'),
        dashes=_get_config('dashes', u'\033[34m-'),
        )

_sun = _get_config('sun', u'\033[33;1m\u2600')
_moon = _get_config('moon', u'\033[36m\u263d')
_clouds = _get_config('clouds', u'\033[37;1m\u2601')
_rain = _get_config('rain', u'\u2614')
_snow = _get_config('snow', u'\u2744')

class Weather(object):
    'Encapsulates simple access to the openweathermap.org API'

    BASE_URI = 'http://api.openweathermap.org/data/2.5'
    WEATHER = 'weather'
    FIND_CITY = 'find'
    DISPLAY_FORMAT = u"""
%(background)s%(text)s Current weather in %(city)s 
%(delimiter)s%(data)s %(temperature)s%(scale)s %(icon)s 
%(dashes)s%(text)s Humidity %(delimiter)s%(data)s %(humidity)s%% 
%(dashes)s%(text)s Pressure %(delimiter)s%(data)s %(pressure)s hPa \033[0m
""".replace('\n','')

    def __init__(self, weather, units = None):
        main = weather.get('main')
        sys = weather.get('sys')

        self.city = weather.get('name')
        self.temperature = main.get('temp')
        self.humidity = main.get('humidity')
        self.pressure = main.get('pressure')

        self.sky = weather.get('weather')[0].get('main')
        self.sunrise = datetime.utcfromtimestamp(sys.get('sunrise'))
        self.sunset = datetime.utcfromtimestamp(sys.get('sunset'))
        now = datetime.utcfromtimestamp(weather.get('dt'))

        self.is_night = now >= self.sunset or now <= self.sunrise

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

    def display(self, symbols = None):
        """Displays the instance as a string using ANSI color codes.

        If symbols is supplied and true, or if the default is true, then unicode symbols are used
        for common weather conditions."""
        if symbols is None:
            symbols = _get_config('symbols', 'true') == 'true'

        args = _print_symbols.copy()
        args.update(dict(
            city=self.city,
            temperature=int(round(self.temperature)),
            humidity=int(round(self.humidity)),
            pressure=int(round(self.pressure)),
            icon=self.icon if symbols else self.sky,
            scale=self.scale,
            ))

        return Weather.DISPLAY_FORMAT % args

    @classmethod
    def get_weather(cls, location = None, units = None, app_id = None):
        """Gets the weather for the specified location.

        The weather will be reported in either metric or imperial units.
        Default values for parameters that are not supplied will be read from a configuration file.

        This returns a Weather object on success or an error string on failure"""

        response = cls._make_request(cls.WEATHER, app_id,
                q=location or _get_config('location'),
                units=units or _get_config('units'))

        if not response.get('main'):
            return response.get('message', 'Unknown error')
        return Weather(response, units)

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
    parser.add_argument('-f', '--find', action='count', help='Lookup city name')
    return parser

if __name__ == '__main__':
    p = _make_parser()
    a = p.parse_args()

    if a.find > 0:
        cities = Weather.find_city(a.location)
        if not cities:
            print "No matching cities were found"
        else:
            for n, c in cities:
                print "%s, %s" % (n, c)
    else:
        w = Weather.get_weather(a.location, a.units)
        if hasattr(w, 'display'):
            print w.display(a.display == 'symbols')
        else:
            print w

