#!/usr/bin/env node
import * as Node from "@lue-bird/gren-state-interface-experimental/node"
// import * as v8 from "node:v8"
import { Gren } from "./grenMain.js"

const grenApp = Gren.Main.init()
// ↓ workers not yet implemented, yet
// ↓ https://github.com/nodejs/node/issues/44014
// v8.startupSnapshot.setDeserializeMainFunction(() => {
Node.programStart({ ports: grenApp.ports })
// })
