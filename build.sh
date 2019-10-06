docker build --target dependencies --cache-from app-dependencies:latest -t app-dependencies .
docker build --target app --cache-from app-dependencies:latest -t telebot .
