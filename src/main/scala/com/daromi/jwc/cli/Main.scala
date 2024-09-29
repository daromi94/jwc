package com.daromi.jwc.cli

import com.daromi.jwc.core.CountOption

val Program: String = "jwc"

@main def main(args: String*): Unit =
  if args.isEmpty && !stdinHasInput then writeUsage()

  val options = parseOptions(args)
  println(options)

  sys.exit(0)

def writeUsage(): Nothing =
  System.err.println(s"usage: $Program [-clw] [file ...]")
  sys.exit(64)

def stdinHasInput: Boolean = System.in.available > 0

def parseOptions(args: Seq[String]): Set[CountOption] =
  val groups = args.takeWhile(arg => arg.length > 1 && arg.startsWith("-"))

  if groups.isEmpty then CountOption.Defaults
  else
    val raw = groups.map(_.substring(1)).flatMap(_.toCharArray)

    val (valid, invalid) = raw.partition(CountOption.exists)

    if invalid.nonEmpty then
      System.err.println(s"$Program: illegal option -- ${invalid.head}")
      writeUsage()
    else valid.flatMap(CountOption.from).toSet
