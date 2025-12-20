import toml
import glob
import subprocess
import os
from pathlib import Path

GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
RESET = "\033[0m"

def compile_program(program: str, compiler_path="../target/debug/jerboa") -> bool:
    """
    Run the compiler to compile the given program.
    """
    result = subprocess.run(
        [compiler_path],
        input=program,
        text=True,
        capture_output=True,
    )

    if result.returncode != 0:
        print(f"{RED}Compilation failed:{RESET}\n {result.stderr}")
        return False

    return True


def run_executable(executable="./a.out") -> subprocess.CompletedProcess:
    """
    Run the executable and capture output.
    """
    env = os.environ.copy()
    env["LD_LIBRARY_PATH"] = str(Path(".").resolve())

    result = subprocess.run(
        ["./a.out"],
        text=True,
        env=env,
        capture_output=True,
    )

    # cleanup
    try:
        Path(executable).unlink()
    except FileNotFoundError:
        pass

    return result


def validate_result(expected: dict, result: subprocess.CompletedProcess) -> bool:
    """
    Validate the actual result against the expected output.
    """
    success = True

    if "exit_code" in expected:
        expected_exit = expected["exit_code"]
        if expected_exit is not None and expected_exit != result.returncode:
            print(f"Exit code mismatch: expected {expected_exit}, found {result.returncode}")
            success = False

    if "stdout" in expected:
        expected_stdout = expected["stdout"]
        if expected_stdout is not None and expected_stdout != result.stdout:
            print(f"Stdout mismatch:\nExpected\n{expected_stdout}\nFound\n{result.stdout}")
            success = False

    if "stderr" in expected:
        expected_stderr = expected["stderr"]
        if expected_stderr is not None and expected_stderr != result.stderr:
            print(f"Stderr mismatch:\nExpected\n{expected_stderr}\nFound\n{result.stderr}")
            success = False

    return success


def run_test(filepath: Path) -> bool:
    """
    Run a single test described in a TOML file.
    """
    data = toml.load(filepath)
    program = data["input"]["program"]
    test_name = data["name"]

    print(f"Running test: {test_name} ({filepath})")

    if not compile_program(program):
        print(f"Result: {RED}Compilation Failed{RESET}")
        return False

    if not data.get("expected"):
        print(f"{YELLOW}No expected outputs provided{RESET}")
        return False

    result = run_executable()
    success = validate_result(data["expected"], result)

    print("Result:", f"{GREEN}Success{RESET}" if success else f"{RED}Failure{RESET}")
    print("-" * 60)

    return True


def main():
    test_files = list(Path(".").glob("*.toml"))
    total = len(test_files)
    passed = sum(run_test(f) for f in test_files)

    overall = f"{GREEN}Success{RESET}" if total == passed else f"{RED}Failure{RESET}"
    print(f"Result: {overall}. {passed} passed; {total - passed} failed.")


if __name__ == "__main__":
    main()
