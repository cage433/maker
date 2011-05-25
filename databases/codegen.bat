@ECHO OFF
@ECHO.

@ECHO CHecking files out of Perforce...
p4 edit //depot/VAR/VarEngine/Head/src/generated/...

@ECHO.

@ECHO Generating ScalaDSLClasses project...
ruby ../../../TradingHub/Core/Head/Tools/BindingGenerator/thubc.rb -b CodegenBindings.rb -o ./src/generated ../../../TradingHub/Model/Head/master-model.rb

@ECHO.

@ECHO Reverting unchanged files in Perforce...
p4 revert -a //depot/VAR/VarEngine/Head/src/generated/...

@ECHO.

PAUSE