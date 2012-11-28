# imports

{ exec } = require 'child_process'


# project variables

srcDir = 'src/coffee'
buildDir = 'site/js'


# convenience function to exec shell commands with output

system = (cmd) -> exec cmd, (err, stdout, stderr) ->
   throw err if err
   console.log stdout + stderr


# build tasks

task 'build', 'Compiles Coffeescript to Javascript', ->
   system "coffee -o #{buildDir} -c #{srcDir}"


task 'clean', 'Clean up build artifacts', ->
   system "rm -rfv #{buildDir}/*"
