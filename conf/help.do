redo-always

exec >&2

echo "--------------------------------------------------------------------------"
echo "--                                NOTICE                                --"
echo "--------------------------------------------------------------------------"
echo "-- This build can be configured using environment variables. Depending  --"
echo "-- on what you are building, you may see various notices about the      --"
echo "-- build configuration. If you want to change the configuration, you    --"
echo "-- can either modify the conf file manually or modify the environment   --"
echo "-- variables when you start the build.                                  --"
echo "--                                                                      --"
echo "-- If you modify the conf file, then it will take precedence over the   --"
echo "-- environemnt variables you might set.  To restore the default         --"
echo "-- environment, just remove the file you modified, it will be           --"
echo "-- regenerated using the environment variables or he default settings.  --"
echo "--------------------------------------------------------------------------"

redo-stamp </dev/null

