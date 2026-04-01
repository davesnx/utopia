import esbuild from "esbuild";
import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import plugin from "server-reason-react-esbuild-plugin";
import { projectPath } from "./paths.mjs";

const configPath = fileURLToPath(import.meta.url);
const runningFromBuildDir = configPath.includes("/_build/default/");

// Derive all paths from the project path
const depth = projectPath ? projectPath.split("/").length : 0;
const utopiaDir = projectPath ? `${projectPath}/_utopia` : "_utopia";
const upSegments = (n) => Array(n).fill("..").join("/");

const nodeModules = runningFromBuildDir
  ? `${upSegments(depth + 2)}/node_modules`
  : depth === 0
    ? "./node_modules"
    : `${upSegments(depth)}/node_modules`;

const sourceBuildRoot = depth === 0
  ? "./_build/default"
  : `${upSegments(depth)}/_build/default`;

const melangeTarget = runningFromBuildDir
  ? `_utopia/target/${utopiaDir}`
  : `${sourceBuildRoot}/${utopiaDir}/target/${utopiaDir}`;

const clientEntryPath = `${melangeTarget}/client_entry_melange.js`;

const outdir = runningFromBuildDir
  ? "_utopia/dist"
  : "./_utopia/dist";

const bootstrapOutput = `${outdir}/bootstrap.js`;
const browserTargetPrefix = `/target/${utopiaDir}`;
const absoluteTargetPrefix = path.join(process.cwd(), melangeTarget).replaceAll("\\", "/");

await esbuild.build({
  entryPoints: [clientEntryPath],
  bundle: true,
  platform: "browser",
  format: "esm",
  splitting: true,
  outdir,
  nodePaths: [nodeModules],
  plugins: [
    plugin({
      target: melangeTarget,
      bootstrapOutput,
      entrypoints: [clientEntryPath],
    }),
  ],
});

const bootstrapContents = await fs.readFile(bootstrapOutput, "utf8");
const rewrittenBootstrap = bootstrapContents
  .replaceAll(absoluteTargetPrefix, browserTargetPrefix)
  .replaceAll(melangeTarget, browserTargetPrefix);

if (rewrittenBootstrap !== bootstrapContents) {
  await fs.writeFile(bootstrapOutput, rewrittenBootstrap, "utf8");
}
