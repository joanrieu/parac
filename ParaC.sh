#!/bin/bash
export CLASSPATH="bin/:/usr/share/java/antlr-complete.jar:$CLASSPATH"
java net.fififox.ParaC.ParaCCompiler "$@"
