require "yaml"

libs = [
  "lib/utils.ml",
  "lib/types.ml",
  "lib/json.ml",
]

dep_map = {
  "bin/test_json" => [*libs, "test/test_json.ml"],
  # "bin/utils_tester" => ["test/utils_tester.pas" , *libs],
  "bin/lexer"   => [*libs, "lexer.ml"],
  "bin/parser"  => [*libs, "parser.ml"],
  "bin/codegen" => [*libs, "codegen.ml"]
}

dep_map.each do |goal, src_files|
  file goal => src_files do |t|
    exe_file = t.name

    src_files.each { |src_file|
      sh %(cp #{src_file} z_tmp/)
    }

    tmp_src_files =
      src_files
        .map { |file| "z_tmp/" + File.basename(file) }
        .join(" ")

    cmd = [
      %( ./docker_run.sh ),
      %( ocamlopt -I z_tmp -o "#{exe_file}" "str.cmxa" ),
      tmp_src_files
    ].join(" ")

    sh cmd
  end
end

task :default => :"build-all"
task :"build-all" => [
  "bin/test_json",
  "bin/lexer",
  "bin/parser",
  "bin/codegen"
]
