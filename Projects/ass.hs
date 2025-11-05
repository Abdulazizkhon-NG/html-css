-- Описание данных для студента
data Student = Student {
    firstName :: String,
    lastName :: String,
    grades :: [Int],
    attendance :: Float
} deriving (Show)

-- Примеры студентов
alice :: Student
alice = Student "Alice" "Smith" [90, 85, 78] 95.0

bob :: Student
bob = Student "Bob" "Lee" [75, 82, 90] 85.0

charlie :: Student
charlie = Student "Charlie" "Brown" [55, 60, 45] 70.0

students :: [Student]
students = [alice, bob, charlie]

-- Перевод числовой оценки в GPA
gradeToGPA :: Int -> Float
gradeToGPA g
    | g >= 90 = 4.0
    | g >= 85 = 3.5
    | g >= 80 = 3.0
    | g >= 75 = 2.5
    | g >= 70 = 2.0
    | g >= 65 = 1.5
    | g >= 60 = 1.0
    | otherwise = 0.0

-- Считаем сумму всех GPA у одного студента
sumGPAPoints :: Student -> Float
sumGPAPoints student =
    -- берём список оценок
    let studentGrades = grades student
        -- переводим каждую оценку в GPA
        gpaList = map gradeToGPA studentGrades
    in sum gpaList
    -- функция map применяет gradeToGPA к каждой оценке по очереди
    -- функция sum складывает все значения в списке

-- Считаем средний GPA
calculateGPA :: Student -> Float
calculateGPA student =
    let allGrades = grades student
        countGrades = length allGrades
        totalPoints = sumGPAPoints student
    in if countGrades == 0
        then 0.0
        else totalPoints / fromIntegral countGrades
    -- fromIntegral нужен, чтобы перевести Int в Float перед делением

-- Фильтруем список студентов по какому-то условию
filterStudents :: (Student -> Bool) -> [Student] -> [Student]
filterStudents rule students =
    filter rule students
    -- filter проходит по списку и оставляет только тех студентов,
    -- для кого правило (rule) возвращает True

-- Проверка по GPA
filterByGPA :: Float -> Student -> Bool
filterByGPA minGPA student =
    calculateGPA student >= minGPA

-- Проверка по посещаемости
filterByAttendance :: Float -> Student -> Bool
filterByAttendance minAtt student =
    attendance student >= minAtt

-- Меняем оценки студента (применяем функцию к каждой)
fmapGrades :: (Int -> Int) -> Student -> Student
fmapGrades change student =
    Student {
        firstName = firstName student,
        lastName = lastName student,
        grades = map change (grades student),
        attendance = attendance student
    }

-- Меняем посещаемость (применяем функцию)
fmapAttendance :: (Float -> Float) -> Student -> Student
fmapAttendance change student =
    Student {
        firstName = firstName student,
        lastName = lastName student,
        grades = grades student,
        attendance = change (attendance student)
    }

-- Главная часть программы
main :: IO ()
main = do
    putStrLn "=== Тест программы ==="

    putStrLn ("Сумма GPA-точек для Alice: " ++ show (sumGPAPoints alice))
    putStrLn ("Средний GPA для Alice: " ++ show (calculateGPA alice))
    putStrLn ("Средний GPA для Bob: " ++ show (calculateGPA bob))
    putStrLn ("Средний GPA для Charlie: " ++ show (calculateGPA charlie))

    let goodGPAStudents = filterStudents (filterByGPA 3.0) students
    putStrLn ("Студентов с GPA >= 3.0: " ++ show (length goodGPAStudents))

    let goodAttendanceStudents = filterStudents (filterByAttendance 80.0) students
    putStrLn ("Студентов с attendance >= 80: " ++ show (length goodAttendanceStudents))

    let bobNew = fmapGrades (+5) bob
    putStrLn ("Новые оценки Bob: " ++ show (grades bobNew))

    let aliceNew = fmapAttendance (+5) alice
    putStrLn ("Новая посещаемость Alice: " ++ show (attendance aliceNew))

    putStrLn "=== Конец ==="
