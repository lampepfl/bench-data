import java.time.{ZonedDateTime, ZoneOffset}

class ImporterSuite extends munit.FunSuite:

  test("parseBenchmarkNameSimple"):
    assertEquals(
      parseBenchmarkName("dotty.tools.benchmarks.CompilationBenchmarks.implicitCacheBootstrapped"),
      "implicitCacheBootstrapped",
      "parseBenchmarkName should return the last part of the method name"
    )

  test("parseBenchmarkNameNightly"):
    assertEquals(
      parseBenchmarkName("dotty.tools.benchmarks.CompilationBenchmarks.dottyNightly"),
      "dotty",
      "parseBenchmarkName should remove the 'Nightly' suffix"
    )

  test("readResultsExample1"):
    assertEquals(
      readJMHResults(os.Path("scripts/tests/example_jmh_output_1.txt", os.pwd)),
      List(
        JMHResults("implicitCacheBootstrapped", List(562.817, 386.297, 365.383), List(366.542, 390.866, 352.338)),
        JMHResults("implicitCacheTasty", List(899.944, 687.595, 625.322), List(616.428, 551.615, 552.089)),
        JMHResults("implicitNumsBootstrapped", List(1934.992, 1389.856, 1440.386), List(1731.511, 1083.199, 886.713)),
        JMHResults("implicitNumsTasty", List(986.655, 755.808, 695.625), List(599.294, 566.847, 468.301)),
        JMHResults("implicitScopeLoop", List(397.422, 398.656, 333.208), List(332.119, 328.872, 339.061))
      )
    )

  test("importResultsExample1"):
    val outputFile = os.temp()

    importResults(
      2,
      true,
      ZonedDateTime.parse("2021-09-01T00:00:01+01:00").withZoneSameInstant(ZoneOffset.UTC),
      "abc123",
      ZonedDateTime.parse("2021-09-01T00:05-02:00").withZoneSameInstant(ZoneOffset.UTC),
      os.Path("scripts/tests/example_jmh_output_1.txt", os.pwd),
      outputFile,
    )

    assertNoDiff(
      os.read(outputFile),
      """
      implicitCacheBootstrapped,2,true,2021-08-31T23:00:01Z,abc123,2021-09-01T02:05:00Z,562.817 386.297 365.383,366.542 390.866 352.338
      implicitCacheTasty,2,true,2021-08-31T23:00:01Z,abc123,2021-09-01T02:05:00Z,899.944 687.595 625.322,616.428 551.615 552.089
      implicitNumsBootstrapped,2,true,2021-08-31T23:00:01Z,abc123,2021-09-01T02:05:00Z,1934.992 1389.856 1440.386,1731.511 1083.199 886.713
      implicitNumsTasty,2,true,2021-08-31T23:00:01Z,abc123,2021-09-01T02:05:00Z,986.655 755.808 695.625,599.294 566.847 468.301
      implicitScopeLoop,2,true,2021-08-31T23:00:01Z,abc123,2021-09-01T02:05:00Z,397.422 398.656 333.208,332.119 328.872 339.061
      """.stripIndent()
    )
