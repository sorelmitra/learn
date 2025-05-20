# Makefile for watching PlantUML files and generating diagrams on changes (macOS, local JAR)
# Run:
#   make watch-my-file
# to watch my-file.puml and generate diagrams with your local plantuml.jar.
#
# Or run:
#  make generate FILE=my-file.puml
# to generate a diagram once.

PLANTUML_JAR ?= /Users/sorel/Applications/plantuml-1.2025.2.jar
IMG_DIR ?= ./images

watch-%:
	fswatch -o $*.puml | xargs -n1 -I{} java -jar $(PLANTUML_JAR) -output $(IMG_DIR) $*.puml

generate:
	java -jar $(PLANTUML_JAR) -output $(IMG_DIR) $(FILE)
