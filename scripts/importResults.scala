import java.time.ZonedDateTime
import com.github.tototoshi.csv.CSVWriter

case class JMHResults(benchmark: String, warmup: Seq[Double], measures: Seq[Double])

@main def importResults(
    prString: String,
    mergedString: String,
    commitTimeString: String,
    commit: String,
    benchTimeString: String,
    jmhOutput: String,
    dataCsv: String
): Unit =
  println(s"pr: $prString")
  println(s"merged: $mergedString")
  println(s"commitTime: $commitTimeString")
  println(s"commit: $commit")
  println(s"benchTime: $benchTimeString")
  println(s"jmhOutputPath: $jmhOutput")
  println(s"dataCsvPath: $dataCsv")
  println(s"pwd: ${os.pwd}")

  val pr = prString.toInt
  val merged = mergedString.toBoolean
  val commitTime = ZonedDateTime.parse(commitTimeString)
  val benchTime = ZonedDateTime.parse(benchTimeString)
  val jmhOutputPath = os.Path(jmhOutput, os.pwd)
  val dataCsvPath = os.Path(dataCsv, os.pwd)
  assert(os.exists(jmhOutputPath), s"`$jmhOutputPath` not found.")
  assert(os.exists(dataCsvPath), s"`$dataCsvPath` not found.")

  val writer = CSVWriter.open(dataCsvPath.toString(), append = true)
  for jmhResults <- readJMHResults(jmhOutputPath) do
    println(s"Write results for benchmark `${jmhResults.benchmark}`")
    val resultsRow = Results(
      jmhResults.benchmark,
      pr,
      merged,
      commitTime,
      commit,
      benchTime,
      jmhResults.warmup,
      jmhResults.measures
    ).toCSVRow()
    writer.writeRow(resultsRow)
  writer.close()

/** Reads results from a JMH text output file. */
def readJMHResults(jmhOutputPath: os.Path): Seq[JMHResults] =
  val benchmarkPrefix = "# Benchmark: "
  val warmupPrefix = "# Warmup Iteration"
  val measurePrefix = "Iteration "
  val lines = os.read.lines(jmhOutputPath)
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
