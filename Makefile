deploy:
	stack --docker clean && stack --docker build --copy-bins && heroku container:push web && heroku container:release web

minikube-build:
	stack --docker clean && stack --docker build --copy-bins && docker build -t website:v1 .

local-run:
	stack clean && stack exec -- env YESOD_PGHOST=localhost yesod devel

local-test:
	stack test
