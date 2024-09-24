package com.daromi.jwc.cli

import com.daromi.jwc.core.{CountByOption, CountOption, count}
import com.daromi.jwc.{FilePath, readBytesAndApply}

@main def main(args: String*): Unit =
  if args.size < 1 then
    writeUsage()
    sys.exit(64)

  val countOptions = parseCountOptions(args(0))

  val filePaths = parseFilePaths(args.toSeq)
  if filePaths.isEmpty then
    writeUsage()
    sys.exit(64)

  val counts = filePaths.flatMap(p => countFile(p, countOptions))

  // TODO: print to stdout
  println(counts)

  sys.exit(0)

def writeUsage(): Unit =
  Console.err.println("usage: jwc [-clw] [file ...]")

def parseCountOptions(arg: String): Set[CountOption] =
  Set(CountOption.Bytes, CountOption.Lines, CountOption.Words)

def parseFilePaths(args: Seq[String]): Seq[FilePath] =
  // TODO: validate paths
  args.filter(a => !a.startsWith("-"))

def countFile(
    filePath: FilePath,
    countOptions: Set[CountOption]
): Option[(FilePath, CountByOption)] =
  val countWithOptions = (b: Array[Byte]) => count(b, countOptions)

  readBytesAndApply(filePath, countWithOptions) match
    case Some(c) => Some((filePath, c))
    case None    => None // TODO: handle i/o exceptions gracefully
