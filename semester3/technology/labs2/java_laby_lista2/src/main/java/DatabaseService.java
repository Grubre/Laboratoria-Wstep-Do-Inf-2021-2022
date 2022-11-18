public class DatabaseService<T> {
    private IDatabaseRepository<T> databaseRepository;
    public DatabaseService(IDatabaseRepository<T> databaseRepository) {
        this.databaseRepository = databaseRepository;
    }
    public void Add(T t) {
        databaseRepository.Add(t);
    }
}
