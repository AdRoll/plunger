from __future__ import print_function
import luigi


class TestTask(luigi.Task):
    foo = luigi.Parameter()
    bar = luigi.Parameter()

    def run(self):
        pass


class OtherTestTask(luigi.Task):

    def requires(self):
        return TestTask(foo="a")

    def run(self):
        pass
