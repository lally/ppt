import minimal


record = minimal.First()


def test_minimal():
    """Currently creates a file called 'minimal_test.txt' with sample output'"""
    # Simulate an attachment...shouldn't need to interact with Minimal directly
    minimal.Minimal.shmat_id = 1

    for i in range(1, 10):
        record.duration_start()
        record.increment_counter()
        record.duration_end()
        record.save()


test_minimal()
