package com.daromi.jwc

import com.daromi.jwc.{CountByOption, FilePath, count, readBytesAndApply}

@main def main(args: String*): Unit =
  if args.isEmpty && !stdinHasInput then writeUsage()

  val options = parseOptions(args)

  val filePaths = parseFilePaths(args)
  if filePaths.isEmpty then
    writeUsage()
    sys.exit(64)

  val counts = filePaths.flatMap(p => countFile(p, options))

  // TODO: print to stdout
  println(counts)

  sys.exit(0)

def stdinHasInput: Boolean = System.in.available > 0

def writeUsage(): Nothing =
  System.err.println("usage: jwc [-clw] [file ...]")
  sys.exit(64)

def parseOptions(args: Seq[String]): Set[CountOption] =
  val groups = args.takeWhile(arg => arg.length > 1 && arg.startsWith("-"))

  if groups.isEmpty then CountOption.DEFAULTS
  else
    val raw = groups.map(_.substring(1)).flatMap(_.toCharArray)

    val (valid, invalid) = raw.partition(CountOption.exists)

    if invalid.nonEmpty then
      System.err.println(s"jwc: illegal option -- ${invalid.head}")
      writeUsage()
    else valid.flatMap(CountOption.from).toSet

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
