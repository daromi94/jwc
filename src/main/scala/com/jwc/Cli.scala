package com.jwc

@main def main(args: String*): Unit =
  if args.size < 1 then
    Console.out.println("usage: jwc [-clw] [file ...]")
    sys.exit(1)

  val options = parseOptions(args(0))
  options.foreach(println)
  sys.exit(0)

def parseOptions(arg: String): Set[CountOption] =
  if arg.startsWith("-") then
    arg.substring(1).toSet.flatMap(c => CountOption.from(c))
  else Set(CountOption.Bytes, CountOption.Lines, CountOption.Words)
