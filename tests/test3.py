status = 0;

def scope(status = 1):
    print(status);
    return status + 1;;

def main():
    print(status);
    x = scope(status);
    return x - 1;;


print(main());
    