package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.Imports
import com.wavesplatform.lang.directives.{Directive, DirectiveParser}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.{CompilationError, ContractCompiler}
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.test.PropSpec

class ContractCompilerWithParserV2Test extends PropSpec {

  def compile(
               script: String,
               lastInsertedCharPos: Option[Int] = None,
               saveExprContext: Boolean = false
             ): Either[String, (Option[DApp], Expressions.DAPP, Iterable[CompilationError])] = {

    val result = for {
      directives <- DirectiveParser(script)
      ds         <- Directive.extractDirectives(directives)
      ctx = lazyContexts(ds.copy(imports = Imports()))().compilerContext
      compResult <- ContractCompiler.compileWithParseResult(script, ctx, ds.stdLibVersion, lastInsertedCharPos = lastInsertedCharPos, saveExprContext = saveExprContext)
    } yield compResult

    result
  }

  property("Parser V2 - simple test") {
    val script = """
                   |{-# STDLIB_VERSION 5 #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |{-# CONTENT_TYPE DAPP #-}
                   |
                   |@Callable(inv)
                   |func default() = {
                   |  [ IntegerEntry("x", inv.caller.wavesBalance().available) ]
                   |}
                   |
                   |""".stripMargin

    val result = compile(script)
    result shouldBe Symbol("right")

    val resultErrorList = result.map(_._3)
    resultErrorList shouldBe Right(Iterable.empty)
  }

  property("Parser V2 - error position test") {
    val script = """
                   |{-# STDLIB_VERSION 5 #-}
                   |{-# CONTENT_TYPE DAPP #-}
                   |{-# SCRIPT_TYPE ACCOUNT #-}
                   |
                   |@Callable(i)
                   |func call() = {
                   |  let asset = Issue("Asset", "", 1, 0, true, unit, 0)
                   |  let assetId = asset.calculateAssetId()

                   |  [
                   |    ScriptTransfer(i.)
                   |  ]
                   |}
                   |
                   |@Verifier(tx)
                   |func verify() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
                   |
                   |""".stripMargin

    val unfinishedExpr = "i."
    val stringWithError = "ScriptTransfer(i.)"

    val result = compile(script, Some(script.indexOf(unfinishedExpr) + 1))
    result shouldBe Symbol("right")

    val partOfScriptWithError = result.map{
      r =>
        r._3
          .map(err => script.substring(err.start, err.end))
          .toList
          .maxBy(_.length)
    }

    partOfScriptWithError shouldBe Right(stringWithError)
  }
}
