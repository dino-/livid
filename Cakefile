# imports

{ exec } = require 'child_process'


# project variables

srcDir = 'src/coffee'
buildDir = 'site/js'


# convenience function to exec shell commands with output

system = (cmd, succFunction) -> exec cmd, (err, stdout, stderr) ->
   console.log stdout
   if (err)
      throw err
   else
      succFunction()


# build tasks

task 'build', 'Compiles CoffeeScript to JavaScript', ->
   system "coffee -o #{buildDir} -c #{srcDir}",
      () -> console.log 'build successful'


task 'clean', 'Clean up build artifacts', ->
   system "rm -rfv #{buildDir}/*", () ->
