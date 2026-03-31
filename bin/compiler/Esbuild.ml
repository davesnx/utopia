let up_segments count = String.concat "/" (List.init count (fun _ -> ".."))

let generate () =
  let project_path = Project.workspace_relative_project_path () in
  let depth = Project.project_path_depth project_path in
  let source_workspace_prefix = if depth = 0 then "." else up_segments depth in
  let project_utopia_dir =
    if project_path = "" then Utopia_path.generated_directory_name
    else
      Printf.sprintf "%s/%s" project_path Utopia_path.generated_directory_name
  in
  let source_build_root =
    if source_workspace_prefix = "." then "./_build/default"
    else source_workspace_prefix ^ "/_build/default"
  in
  let source_melange_target_dir =
    Printf.sprintf "%s/%s/target/%s" source_build_root project_utopia_dir
      project_utopia_dir
  in
  let build_melange_target_dir =
    Filename.concat "_utopia/target" project_utopia_dir
  in
  let source_node_modules_dir =
    if source_workspace_prefix = "." then "./node_modules"
    else source_workspace_prefix ^ "/node_modules"
  in
  let build_node_modules_dir = up_segments (depth + 2) ^ "/node_modules" in
  let source_outdir = "./" ^ Utopia_path.generated_directory_name ^ "/dist" in
  let build_outdir = Utopia_path.generated_directory_name ^ "/dist" in
  let source_bootstrap_output = source_outdir ^ "/bootstrap.js" in
  let build_bootstrap_output = build_outdir ^ "/bootstrap.js" in
  Printf.sprintf
    {|
import esbuild from "esbuild";
import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import plugin from "server-reason-react-esbuild-plugin";

const configPath = fileURLToPath(import.meta.url);
const runningFromBuildDir = configPath.includes("/_build/default/");
const nodeModules = runningFromBuildDir
  ? %S
  : %S;
const melangeTarget = runningFromBuildDir
  ? %S
  : %S;
const clientEntryPath = `${melangeTarget}/client_entry_melange.js`;
const outdir = runningFromBuildDir
  ? %S
  : %S;
const bootstrapOutput = runningFromBuildDir
  ? %S
  : %S;
const browserTargetPrefix = %S;
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
|}
    build_node_modules_dir source_node_modules_dir build_melange_target_dir
    source_melange_target_dir build_outdir source_outdir build_bootstrap_output
    source_bootstrap_output
    ("/target/" ^ project_utopia_dir)
