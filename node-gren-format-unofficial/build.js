import * as grenEsm from "gren-esm"
import * as fs from "node:fs"
import * as grenOptimizeLevel2 from "gren-optimize-level-2"

const grenMainFilePath = "src/Main.gren"
const compiledGrenMainJsFilePath = "src/grenMain.js"

await grenOptimizeLevel2.run({
    inputFilePath: [grenMainFilePath],
    outputFilePath: compiledGrenMainJsFilePath,
    optimizeSpeed: true,
    processOpts: null
})
const optimizedCompiledCode = await fs.promises.readFile(compiledGrenMainJsFilePath, { encoding: "utf-8" })

const optimizedCompiledCodeAsEsm = grenEsm.toESModule(optimizedCompiledCode)

await fs.promises.writeFile(compiledGrenMainJsFilePath, optimizedCompiledCodeAsEsm, { encoding: "utf-8" })
