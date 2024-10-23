# darwin_core_viewer


A basic Shiny app to display (map and plots) biodiversity data managed with Darwin Core data format. Meant as a generic starting point to customize more sophisticated apps.

![Screenshot of the Shiny app to build a Darwin_Core_viewer](./www/Shiny_Darwin_Core_viewer.png)


## Running the app from docker

```
docker pull ghcr.io/firms-gta/darwin_core_viewer:latest
docker run --name darwin_core_viewer -p 3838:3838 ghcr.io/firms-gta/darwin_core_viewer
```

And then point your browser to http://localhost:3838

Note: In case of having an alreday existing shiny_compare_tunaatlas_datasests app running on docker, and in order to update the docker app, it will be required to stop and remove the container prior to run the above commands to pull & run the app:

```
docker container stop darwin_core_viewer-cache
docker container rm darwin_core_viewer-cache
```

#### Build / Run the image locally

A Dockerfile is provided on GHCR and can be used to build up containers with the application.

To build and run the application issue the following commands
```
sudo docker build -t darwin_core_viewer-cache <Path of the Dockerfile>
sudo docker run -p 3839:3838 darwin_core_viewer-cache
```

And then point your browser to http://localhost:3839
