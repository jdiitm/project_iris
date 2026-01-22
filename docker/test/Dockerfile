# =============================================================================
# Project Iris - Deterministic Test Environment
# =============================================================================
# This Dockerfile creates a hermetic test environment that produces identical
# results across any host system (x86, ARM, Linux, macOS).
#
# Build:   docker build -t iris-test -f docker/test/Dockerfile .
# Run:     docker run --rm iris-test
# Seed:    docker run --rm -e TEST_SEED=12345 iris-test
# =============================================================================

FROM erlang:25-alpine

# Install Python and dependencies
RUN apk add --no-cache \
    python3 \
    py3-pip \
    bash \
    curl \
    procps \
    coreutils \
    && rm -rf /var/cache/apk/*

# Create app directory
WORKDIR /app

# Copy requirements first (for layer caching)
COPY requirements-test.txt /app/
RUN pip3 install --no-cache-dir --break-system-packages -r requirements-test.txt

# Copy source code
COPY . /app

# Set up determinism environment variables
ENV PYTHONUNBUFFERED=1
ENV PYTHONDONTWRITEBYTECODE=1
ENV TEST_SEED=42
ENV TEST_RUN_ID=docker_run
ENV CI=true
ENV TERM=xterm-256color

# Compile Erlang code
RUN make all

# Health check - verify Erlang and Python are working
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD erl -noshell -eval "io:format('ok'), init:stop()." && python3 --version

# Default command: run all tests
CMD ["python3", "-u", "tests/run_tests.py", "--all"]

# Labels
LABEL maintainer="Project Iris"
LABEL description="Deterministic test environment for Project Iris"
LABEL version="1.0"
