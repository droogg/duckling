# Используйте официальный образ Haskell с Debian Buster
FROM haskell:8-buster

# Установите необходимые системные зависимости
RUN apt-get update -qq && \
    apt-get install -qq -y libpcre3 libpcre3-dev build-essential pkg-config --fix-missing --no-install-recommends && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Создайте директорию для логов (если она вам необходима)
RUN mkdir /log

# Установите рабочую директорию для проекта
WORKDIR /duckling

# Установите переменную окружения LANG
ENV LANG=C.UTF-8

# Копируйте файлы проекта в контейнер
#COPY . .

# Запустите stack setup для установки подходящей версии GHC
# В зависимости от вашего stack.yaml
# RUN stack setup

# Вы можете запустить сборку проекта здесь, если это нужно при сборке образа
# RUN stack build

# Задайте команду по умолчанию для контейнера
# Например, запуск REPL или сервера
# CMD ["stack", "repl"]
