package com.daromi.jwc

import java.nio.file.{Files, Path}

type FilePath = String

def readBytesAndApply[T](
    filePath: FilePath,
    f: Array[Byte] => T
): Option[T] =
  val bytes =
    try
      val path = Path.of(filePath)
      Some(Files.readAllBytes(path))
    catch case _: Exception => None // TODO: handle exceptions expressively

  bytes.map(f)
