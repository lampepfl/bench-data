//> using dep com.github.tototoshi::scala-csv:2.0.0
//> using dep "com.lihaoyi::os-lib:0.10.4"

import com.github.tototoshi.csv.{CSVReader, CSVWriter}

@main def main(): Unit =
  val out = os.pwd / "vizualizer" / "data"
  os.remove.all(out)
  os.makeDir.all(out / "detailed")
  os.makeDir.all(out / "aggregated" / "last100")
  os.makeDir.all(out / "aggregated" / "all")

  case class AggregatedRow(benchmark: String, commitTime: String, commit: String, pr: String, min: Double, median: Double, max: Double)
  val aggregated = collection.mutable.ArrayBuffer.empty[AggregatedRow]

  if !os.exists(os.pwd / "history2.csv") then
    println("`history2.csv` not found. This script should be run from the root of the project.")
    return

  // Compute aggregated rows and write detailed rows
  val reader = CSVReader.open("history2.csv")
  for row <- reader.iterator do
    val Seq(benchmark, pr, commitTime, commit, benchTime, warmupRaw, measuresRaw) = row
    val measures = IArray.from(measuresRaw.trim().split(" ").map(_.toDouble)).sorted
    val size = measures.length
    val median = if size % 2 == 0 then (measures(size / 2) + measures(size / 2 - 1)) / 2 else measures(size / 2)
    aggregated += AggregatedRow(benchmark, commitTime, commit, pr, measures.head, median, measures.last)
    val writer = CSVWriter.open(s"vizualizer/data/detailed/$commit.csv", append = true)
    writer.writeRow(Seq(benchmark, benchTime, warmupRaw, measuresRaw))
    writer.close()
  reader.close()

  // Write aggregated rows
  for (benchmark, rows) <- aggregated.groupBy(_.benchmark) do
    def writeRows(rows: collection.Seq[AggregatedRow], folder: String) =
      val writer = CSVWriter.open(s"vizualizer/data/aggregated/$folder/$benchmark.csv")
      for row <- rows do
        writer.writeRow(Seq(row.commitTime, row.commit, row.pr, row.min, row.median, row.max))
      writer.close()
    val sortedRows = rows.sortBy(_.commitTime)
    writeRows(sortedRows, "all")
    writeRows(sortedRows.takeRight(100), "last100")
