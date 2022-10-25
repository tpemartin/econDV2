serveJupyterNotebook = function(){
  rstudioapi::terminalExecute(
    "jupyter notebook \
  --NotebookApp.allow_origin='https://colab.research.google.com' \
  --port=4321 \
  --NotebookApp.port_retries=0"
  )
}
