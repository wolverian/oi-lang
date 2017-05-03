.PHONY: usage
usage:
	@echo
	@echo Usage:
	@echo "	make dev	- Automatic development build"
	@echo "	make prod	- Production build"
	@echo "	make test	- Run tests once"
	@echo "	make test-auto	- Run tests automatically"
	@echo "	make lint	- Check code quality"
	@echo "	make deploy-site	- Deploy the site"
	@echo

.PHONY: dev
dev:
	rlwrap lein figwheel

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

.PHONY: test-auto
test-auto:
	lein karma-once # this is needed so that the files are there when karma starts watching them
	tmux new-session -d \
		'karma start' \; \
		split-window -h 'lein karma-auto' \; \
		attach

.PHONY: lint
lint:
	lein kibit

.PHONY: deploy-site
deploy-site: prod
	now --name oi-lang resources/public/
	now alias
