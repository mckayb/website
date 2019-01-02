build:
	stack --docker clean && stack --docker build --copy-bins

deploy:
	stack --docker clean && stack --docker build --copy-bins && heroku container:push web && heroku container:release web
