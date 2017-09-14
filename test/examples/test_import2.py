import luigi


class TestTask(luigi.Task):
    foo = luigi.Parameter()
    bar = luigi.Parameter()

    def run(self):
        pass
