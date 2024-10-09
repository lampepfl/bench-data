import java.time.ZonedDateTime
import com.github.tototoshi.csv.CSVWriter

case class JMHResults(benchmark: String, warmup: Seq[Double], measures: Seq[Double])

@main def importResults(
    pr: String,
    merged: String,
    commitTime: String,
    commit: String,
    benchTimeRaw: String,
    jmhOutputPath: String,
    dataCsvPath: String
): Unit =
  if !os.exists(os.Path(dataCsvPath, os.pwd)) then
    throw new IllegalArgumentException(s"`$dataCsvPath` not found.")

  val benchTime = if benchTimeRaw == "now" then java.time.Instant.now().toString() else benchTimeRaw
  val writer = CSVWriter.open(dataCsvPath, append = true)
  for jmhResults <- readJMHResults(jmhOutputPath) do
    val resultsRow = Results(
      jmhResults.benchmark,
      pr.toInt,
      merged.toBoolean,
      ZonedDateTime.parse(commitTime),
      commit,
      ZonedDateTime.parse(benchTime),
      jmhResults.warmup,
      jmhResults.measures
    ).toCSVRow()
    writer.writeRow(resultsRow)
  writer.close()

/** Reads results from a JMH text output file. */
def readJMHResults(jmhOutputPath: String): Seq[JMHResults] =
  val benchmarkPrefix = "# Benchmark: "
  val warmupPrefix = "# Warmup Iteration"
  val measurePrefix = "Iteration "
  val lines = os.read.lines(os.Path(jmhOutputPath, os.pwd))
  val results = collection.mutable.ArrayBuffer.empty[JMHResults]
  var benchmark = ""
  var warmup = collection.mutable.ArrayBuffer.empty[Double]
  var measures = collection.mutable.ArrayBuffer.empty[Double]
  for line <- lines do
    if line.startsWith(benchmarkPrefix) then
      if benchmark.nonEmpty then
        results += JMHResults(benchmark, warmup.toSeq, measures.toSeq)
        warmup.clear()
        measures.clear()
      benchmark = parseBenchmarkName(readValue(line))
    if line.startsWith(warmupPrefix) then
      warmup += parseTime(readValue(line))
    if line.startsWith(measurePrefix) then
      measures += parseTime(readValue(line))
  results += JMHResults(benchmark, warmup.toSeq, measures.toSeq)
  results.toSeq

/** Reads the value of a line that has the format `key: value`. */
def readValue(line: String): String =
  val parts = line.split(":")
  assert(parts.length == 2, s"expected 2 parts separated by ':' in line '$line'")
  parts(1).trim

/** Parses a benchmark method name into a short name. */
def parseBenchmarkName(methodName: String): String =
  val nightlySuffix = "Nightly"
  val name = methodName.split("\\.").last
  if name.endsWith(nightlySuffix) then name.dropRight(nightlySuffix.length) else name

/** Parses a time value from a JMH output line. It must end with 'ms/op'. */
def parseTime(time: String): Double =
  val timeUnit = " ms/op"
  assert(time.endsWith(timeUnit), s"expected $time to end with time unit '$timeUnit'")
  time.dropRight(timeUnit.length).toDouble
