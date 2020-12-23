FROM haskell:8
WORKDIR /app
COPY . .
RUN stack setup
RUN stack build
ENTRYPOINT ["stack", "exec", "lambda-calculus-exe"]
