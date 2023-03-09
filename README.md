# Basic dev container for R

Before you run it, make a `.env` file with:

```env
WORKING_DIR="path/to/your/work-directory"
RENV_CACHE_PATH="path/to/your/local/renv/cache"
```

The renv cache of the container will be mounted on your local cache, in order to speed up (reuse and recycle) the installation of R packages.

# Build the image

Build the image with:

```sh
docker-compose up --build
```

# Use the dev container

Open the folder in Visual Studio Code and select [Open in Container](https://code.visualstudio.com/docs/devcontainers/containers#_quick-start-open-an-existing-folder-in-a-container)