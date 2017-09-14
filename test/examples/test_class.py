import json


def some_function(foo):
    return {}


class SomeConfig:
    def __init__(self, s3_path):
        self._config = json.loads("{}")

        if 'version' not in self._config:
            self._config = some_function(self._config)

    def get_all_bar(self):
        return self._config.get('bar').keys()

    def get_bar(self, eid):
        return self._config.get('bar').get(eid)
