import toml
import glob
import subprocess

def compile_and_run(program):
    result = subprocess.run(
        ["../target/debug/jerboa", "-v"],
        input=program,
        text=True,
        capture_output=True,
    )

    if result.returncode != 0:
        return False

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

    if prog_result == False:
        test_pass = False
        result = "Compilation Failed"
    elif data["expected"] is None:
        print("No expected outputs provided")
        test_pass = False
        result = "Success"
    else:
        test_pass = validate(data["expected"], prog_result)
        result = "Success" if test_pass else "Failure"

    print(f"Testing {data['name']} ({filename}) .......... {result}")

    return test_pass


tests = glob.glob("./*.toml")

total = 0
success = 0
for file in tests:
    total += 1
    if test(file):
        success += 1

result = "Success" if total == success else "Failure"
print(f"testing result: {result}. {success} passed; {total - success} failed.")
