.PHONY: default
default:
	@echo Usage:
	@echo make dev	- Automatic development build
	@echo make prod	- Production build
	@echo make test	- Run tests
	@echo make lint	- Check code quality	
	@echo make deploy	- Deploy

.PHONY: dev
dev:
	lein figwheel

.PHONY: clean
clean:
	lein clean

.PHONY: prod
prod: clean
	lein cljsbuild once production

.PHONY: test
test:
	lein karma-once
	karma start --single-run --reporters junit,dots

.PHONY: lint
lint:
	lein kibit

.PHONY: deploy
deploy: prod
	now --static resources/public/
