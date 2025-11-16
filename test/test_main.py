import toml
import subprocess

def compile_and_run(program):
    subprocess.run(
        ["../target/debug/pl"],
        input=program,
        text=True,
        capture_output=True,
    )

    result = subprocess.run(
        ["./a.out"],
        text=True,
        capture_output=True,
    )

    subprocess.run(["rm", "a.out"])

    return result

def validate(expected, result):
    exit_code = expected["exit_code"]
    validation = True
    if (exit_code is not None) and (exit_code != result.returncode):
        print(f"Expected exit_code={exit_code}, found {result.returncode} instead")
        validation = False

    stdout = expected["stdout"]
    if (stdout is not None) and (stdout != result.stdout):
        print(f"Expected the following stdout:\n{stdout}\nFound this instead:\n{result.stdout}")
        validation = False

    return validation

def test(filename):
    data = toml.load(filename)

    program = data["input"]["program"]

    prog_result = compile_and_run(program)

    if data["expected"] is None:
        print("No expected outputs provided")
        result = "Success"
    else:
        result = "Success" if validate(data["expected"], prog_result) else "Failure"

    print(f"Testing {data["name"]} ({filename}) .......... {result}")


test("./test-0.toml")
