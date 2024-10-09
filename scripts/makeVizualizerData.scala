import java.time.ZonedDateTime
import com.github.tototoshi.csv.{CSVReader, CSVWriter}

@main def makeVizualizerData(dataCsv: String, vizualizerDir: String): Unit =
  val dataCsvPath = os.Path(dataCsv, os.pwd)
  val vizualizerDirPath = os.Path(vizualizerDir, os.pwd)
  assert(os.exists(dataCsvPath), s"`$dataCsvPath` not found.")
  assert(os.exists(vizualizerDirPath), s"`$vizualizerDirPath` not found.")
  
  val out = vizualizerDirPath / "data"
  os.remove.all(out)
  os.makeDir.all(out / "detailed")
  os.makeDir.all(out / "aggregated" / "last100")
  os.makeDir.all(out / "aggregated" / "all")

  case class AggregatedRow(benchmark: String, commitTime: ZonedDateTime, commit: String, pr: Int, min: Double, median: Double, max: Double)
  val aggregated = collection.mutable.ArrayBuffer.empty[AggregatedRow]

  // Write detailed rows and compute aggregated rows
  val reader = CSVReader.open(dataCsvPath.toString())
  var count = 0
  for row <- reader.iterator do
    val res = Results.fromCSVRow(row)
    val writer = CSVWriter.open((out / "detailed" / "${res.commit}.csv").toString(), append = true)
    writer.writeRow(Seq(res.benchmark, res.benchTime, res.warmup.mkString(" "), res.measures.mkString(" ")))
    writer.close()
    count += 1
    if res.merged then
      val sorted = res.measures
      val size = sorted.length
      val median = if size % 2 == 0 then (sorted(size / 2) + sorted(size / 2 - 1)) / 2 else sorted(size / 2)
      aggregated += AggregatedRow(res.benchmark, res.commitTime, res.commit, res.pr, sorted.head, median, sorted.last)
  println(s"Wrote ${count} detailed rows to `$out/detailed`.")
  reader.close()

  // Write aggregated rows
  for (benchmark, rows) <- aggregated.groupBy(_.benchmark) do
    def writeRows(rows: collection.Seq[AggregatedRow], folder: String) =
      val writer = CSVWriter.open((out / "aggregated" / folder / s"$benchmark.csv").toString())
      for row <- rows do
        writer.writeRow(Seq(row.commitTime.toString(), row.commit, row.pr, row.min, row.median, row.max))
      writer.close()
    val sortedRows = rows.sortBy(_.commitTime)
    writeRows(sortedRows, "all")
    writeRows(sortedRows.takeRight(100), "last100")
  println(s"Wrote ${aggregated.length} aggregated rows to `$out/aggregated`.")

extension (self: Seq[Double]) def median(): Double =
  val sorted = self.sorted
  val size = sorted.length
  if size % 2 == 0 then (sorted(size / 2) + sorted(size / 2 - 1)) / 2 else sorted(size / 2)
