find . -name "*.feature" | xargs \
  xreq \
    --quiet \
    --progress \
    --executable xreq_tests \
    --step step_definitions \
    --output obj \
    --fill-steps-in Generated_Steps >&2

