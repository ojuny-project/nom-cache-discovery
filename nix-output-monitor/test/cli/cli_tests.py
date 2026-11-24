#!/usr/bin/env python3
"""Run the nix-output-monitor executable against a loopback cache proxy."""

import http.server
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import threading
import time

REPO = pathlib.Path(os.environ.get("NCD_REPO", "."))
NOM = os.environ["NOM_BIN"]
HASH = "0123456789abcdef0123456789abcdef"
INFO_PATH = "/.nix-cache-discovery/v1/info"
SOURCES_PATH = "/.nix-cache-discovery/v1/sources"
PROXY_INFO = b"version=1\nid=abc\n"


class Bodies:
    def __init__(self):
        self.lock = threading.Lock()
        self.info = b"hello\n"
        self.sources = b""
        self.requests = []

    def add(self, path):
        with self.lock:
            self.requests.append(path)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self.server.bodies.add(self.path)
        if self.path == INFO_PATH:
            body = self.server.bodies.info
        elif self.path == SOURCES_PATH:
            body = self.server.bodies.sources
        else:
            body = b""
        self.send_response(200)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *_):
        pass


def start_server():
    bodies = Bodies()
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    server.bodies = bodies
    threading.Thread(target=server.serve_forever, daemon=True).start()
    return server, bodies


def request_count(bodies, path):
    with bodies.lock:
        return sum(1 for request in bodies.requests if request == path)


def download_line(port, hash_text=HASH):
    line = (
        '@nix {"action":"start","id":1,"level":3,"text":"copying path",'
        f'"type":100,"fields":["/nix/store/{hash_text}-example","http://127.0.0.1:{port}","local"]}}'
    )
    return (line + "\n").encode()


def old_style_line(port, hash_text=HASH):
    return f"copying path '/nix/store/{hash_text}-example' from 'http://127.0.0.1:{port}'...\n".encode()


def stub_nix(directory, port):
    path = directory / "nix"
    path.write_text(
        "#!/bin/sh\n"
        'if [ "$1" = "eval" ]; then printf x86_64-linux; exit 0; fi\n'
        'if [ "$1" = "build" ]; then cat "$NCD_DOWNLOAD_LINE" >&2; sleep 2; exit 0; fi\n'
        "exit 0\n"
    )
    path.chmod(0o755)


def base_environment(directory, port):
    (directory / "download.json").write_bytes(download_line(port))
    environment = os.environ.copy()
    environment["PATH"] = f"{directory}:{environment['PATH']}"
    environment["NCD_DOWNLOAD_LINE"] = str(directory / "download.json")
    return environment


def run_nom(arguments, stdin_bytes, hold, environment):
    process = subprocess.Popen(
        [NOM, *arguments],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=environment,
    )
    assert process.stdin is not None
    process.stdin.write(stdin_bytes)
    process.stdin.flush()
    time.sleep(hold)
    process.stdin.close()
    stdout = process.stdout.read() if process.stdout else b""
    stderr = process.stderr.read() if process.stderr else b""
    return process.wait(timeout=15), stdout, stderr


# INVENTORY: cli.1.1
def test_one_resolver_per_run():
    server, bodies = start_server()
    try:
        port = server.server_address[1]
        with tempfile.TemporaryDirectory() as directory_name:
            directory = pathlib.Path(directory_name)
            environment = base_environment(directory, port)
            for _ in range(2):
                code, _, stderr = run_nom(["--json"], download_line(port), 2.5, environment)
                assert code == 0, f"nom exited {code}: {stderr[-400:]!r}"
            requests = request_count(bodies, INFO_PATH)
            assert requests == 2, f"expected one info request per run, got {requests}"
    finally:
        server.shutdown()


# INVENTORY: cli.1.2
def test_resolver_keeps_the_failed_build():
    server, bodies = start_server()
    try:
        bodies.info = PROXY_INFO
        bodies.sources = f"{HASH} https://cache.nixos.org\n".encode()
        port = server.server_address[1]
        with tempfile.TemporaryDirectory() as directory_name:
            directory = pathlib.Path(directory_name)
            environment = base_environment(directory, port)
            fail_replay = (REPO / "nix-output-monitor/test/integration/fail/stderr.json").read_bytes()
            code, _, stderr = run_nom(["--json"], fail_replay + download_line(port), 2.5, environment)
            assert code == 1, f"expected the failed build to exit 1, got {code}"
            assert b"cache.nixos.org" in stderr, "the resolved host was not printed"
    finally:
        server.shutdown()


# INVENTORY: cli.1.3
def test_old_style_and_json_commands_get_the_resolver():
    server, bodies = start_server()
    try:
        port = server.server_address[1]
        with tempfile.TemporaryDirectory() as directory_name:
            directory = pathlib.Path(directory_name)
            environment = base_environment(directory, port)
            old_code, _, old_stderr = run_nom([], old_style_line(port), 2.5, environment)
            json_code, _, json_stderr = run_nom(["--json"], download_line(port), 2.5, environment)
            assert old_code == 0, f"old-style nom exited {old_code}: {old_stderr[-400:]!r}"
            assert json_code == 0, f"json nom exited {json_code}: {json_stderr[-400:]!r}"
            requests = request_count(bodies, INFO_PATH)
            assert requests == 2, f"expected one info request per command, got {requests}"
    finally:
        server.shutdown()


# INVENTORY: cli.1.4
def test_printed_state_is_resolved_and_exit_is_failed():
    server, bodies = start_server()
    try:
        bodies.info = PROXY_INFO
        bodies.sources = f"{HASH} https://cache.nixos.org\n".encode()
        port = server.server_address[1]
        with tempfile.TemporaryDirectory() as directory_name:
            directory = pathlib.Path(directory_name)
            environment = base_environment(directory, port)
            fail_replay = (REPO / "nix-output-monitor/test/integration/fail/stderr.json").read_bytes()
            code, _, stderr = run_nom(["--json"], fail_replay + download_line(port), 2.5, environment)
            assert code == 1, f"expected exit 1, got {code}"
            hosts = re.findall(rb"127\.0\.0\.1:\d+|cache\.nixos\.org", stderr)
            assert hosts, "no host token was printed"
            assert hosts[-1] == b"cache.nixos.org", f"the last host token is {hosts[-1]!r}"
    finally:
        server.shutdown()


# INVENTORY: cli.1.5
def test_nix_errors_survive_the_resolver():
    server, bodies = start_server()
    try:
        bodies.info = PROXY_INFO
        bodies.sources = f"{HASH} https://cache.nixos.org\n".encode()
        port = server.server_address[1]
        with tempfile.TemporaryDirectory() as directory_name:
            directory = pathlib.Path(directory_name)
            environment = base_environment(directory, port)
            error_line = b'@nix {"action":"msg","level":0,"msg":"error: boom"}\n'
            code, _, stderr = run_nom(["--json"], error_line + download_line(port), 2.5, environment)
            assert code == 1, f"expected the nix error to exit 1, got {code}"
            assert b"cache.nixos.org" in stderr, "the resolved host was not printed"
    finally:
        server.shutdown()


# INVENTORY: cli.1.6
def test_monitored_subcommand_gets_the_resolver():
    server, bodies = start_server()
    try:
        port = server.server_address[1]
        with tempfile.TemporaryDirectory() as directory_name:
            directory = pathlib.Path(directory_name)
            environment = base_environment(directory, port)
            stub_nix(directory, port)
            code, _, stderr = run_nom(["build", "example"], b"", 3.0, environment)
            assert code == 0, f"nom build exited {code}: {stderr[-400:]!r}"
            requests = request_count(bodies, INFO_PATH)
            assert requests == 1, f"expected one info request from the subcommand, got {requests}"
    finally:
        server.shutdown()


def main():
    tests = [
        ("cli.1.1 one resolver per run", test_one_resolver_per_run),
        ("cli.1.2 the failed build survives the resolver", test_resolver_keeps_the_failed_build),
        ("cli.1.3 old-style and JSON commands get the resolver", test_old_style_and_json_commands_get_the_resolver),
        ("cli.1.4 the printed state is resolved and the exit is failed", test_printed_state_is_resolved_and_exit_is_failed),
        ("cli.1.5 nix errors survive the resolver", test_nix_errors_survive_the_resolver),
        ("cli.1.6 the monitored subcommand gets the resolver", test_monitored_subcommand_gets_the_resolver),
    ]
    failures = 0
    for name, test in tests:
        try:
            test()
            print(f"ok {name}")
        except Exception as error:
            failures += 1
            print(f"FAIL {name}: {error}", file=sys.stderr)
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
