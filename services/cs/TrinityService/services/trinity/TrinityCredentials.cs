namespace com.trafigura.services.trinity
{
    public class TrinityCredentials
    {
        public readonly string Database;
        public readonly string Username;
        public readonly string Password;

        public TrinityCredentials(string database, string username, string password)
        {
            Database = database;
            Username = username;
            Password = password;
        }
    }
}