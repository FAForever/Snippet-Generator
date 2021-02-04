
REM generate from function-based files
REM function GetMarker(name) (...) end
call function-alt.bat   function/AiAttackUtilities.lua AIAttackUtils
call function.bat       function/ScenarioFramework.lua
call function.bat       function/ScenarioPlatoonAI.lua
call function-alt.bat   function/ScenarioTriggers.lua TriggersFile
call function.bat       function/ScenarioUtilities.lua
call function-alt.bat   function/sim.lua ""
call function-alt.bat   function/core.lua ""

REM generate from class-based files
REM GetMarker = function(name) (...) end
call class.bat class/aibrain.lua brain
call class.bat class/entity.lua entity
call class.bat class/platoon.lua platoon
call class.bat class/projectile.lua projectile
call class.bat class/prop.lua prop
call class.bat class/shield.lua shield
call class.bat class/unit.lua unit
call class.bat class/weapon.lua weapon

REM generate from metatable-based files
REM function ScenarioUtils:GetMarker(name) (...) end
call metatable-alt.bat metatable/caibrain.lua brain
call metatable-alt.bat metatable/centity.lua entity
call metatable-alt.bat metatable/cplatoon.lua platoon
call metatable-alt.bat metatable/cprojectile.lua projectile
call metatable-alt.bat metatable/cprop.lua prop
call metatable-alt.bat metatable/cunit.lua unit
call metatable-alt.bat metatable/cunitweapon.lua weapon

REM generate from table-based files
REM function ScenarioUtils.GetMarker(name) (...) end
call table.bat table/utils.lua

REM copy it all to one folder for ease of use
for /R function %%f in (*.code-snippets) do copy %%f output
for /R class %%f in (*.code-snippets) do copy %%f output
for /R metatable %%f in (*.code-snippets) do copy %%f output
for /R table %%f in (*.code-snippets) do copy %%f output
for /R manual %%f in (*.code-snippets) do copy %%f output