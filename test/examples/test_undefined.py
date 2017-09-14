from __future__ import print_function
import luigi


class TestTask(luigi.Task):
    foo = luigi.Parameter()
    bar = luigi.Parameter()

    def run(self):
        print(self.y)
