from SUITE.tutils import maybe_valgrind

print("tool versions are now emitted as comments attached to this run")

print('valgrind control: ' + str(maybe_valgrind([])))

print('End of Z999-999')

# This test will fail by not calling thistest.result()
