deploy:
	stack --docker clean && stack --docker build --copy-bins && heroku container:push web && heroku container:release web

kubernetes-build:
	stack --docker clean && stack --docker build --copy-bins && docker build -t website:v1 .

local-run:
	stack exec -- env YESOD_PGHOST=localhost yesod devel

local-test:
	stack test
