# Autor Jakub VLk
# Projekt: FLP logická část

SWIPL = swipl
PLFLAGS = -q -g start -G16g
TARGET = flp24-log
SOURCE = solution.pl

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(SWIPL) $(PLFLAGS) -o $(TARGET) -c $(SOURCE)

clean:
	rm -f $(TARGET)
