docker run --rm -it -v /tmp/.X11-unix/:/tmp/.X11-unix/ -v ${PWD}:/workspace/master --workdir /workspace/master -e DISPLAY=:0 --device /dev/dri:/dev/dri simp bash
