import luigi
from test_import2 import TestTask


class OtherTestTask(luigi.Task):

    def requires(self):
        return TestTask(foo="a")

    def run(self):
        pass
