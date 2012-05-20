package structural

object BridgeDemo {

  def main(args: Array[String]): Unit = {
    withclasses.BridgeDemo.demoFactory()
    withclasses.BridgeDemo.demoImplicitFileStorage()
    withclasses.BridgeDemo.demoImplicitDBStorage()
    withtraits.BridgeDemo.demo()
  }
}

package withclasses {
  trait Storage {
    def read(key: String): String
    def write(key: String, value: String): Unit
  }
  
  class FileStorage extends Storage {
    def read(key: String) = {
      println("Reading " + key + " from file storage")
      "read_from_file_result"
    }
    def write(key: String, value: String) {
      println("Writing " + value + " for " + key + " to file storage")
    }
  }
  
  class DatabaseStorage extends Storage {
    def read(key: String) = {
      println("Reading " + key + " from database")
      "read_from_database_result"
    }
    def write(key: String, value: String) {
      println("Writing " + value + " for " + key + " to database")
    }
  }
  
  trait CacheDriver {
    def get(key: String): Option[String]
    def put(key: String, value: String): Unit
  }
  
  class SingleCache extends CacheDriver {
    var cachedKey: String = _
    var cachedValue: String = _
    
    def get(key: String) = {
      if (key == cachedKey) {
        println("Found " + cachedValue + " for " + key + " in cache")
        Some(cachedValue) 
      } else None
    }
    def put(key: String, value: String) {
      println("Caching " + value + " for " + key)
      cachedKey = key
      cachedValue = value
    }
  }
  
  package factory {
    object Storage {
      var storage: Storage = _
    }
    
    class Persistence {
      private val storage = Storage.storage
      def load(key: String) = storage.read(key)
      def save(key: String, value: String) = storage.write(key, value)
    }
  
    class CachedPersistence(cache: CacheDriver) extends Persistence {
      override def load(key: String) = {
        cache.get(key) match {
          case Some(value) => value
          case None => {
            val value = super.load(key)
            cache.put(key, value)
            value
          }
        }
      }
      override def save(key: String, value: String) {
        cache.put(key, value)
        super.save(key, value)
      }
    }
  }
  
  package implicits {
    class Persistence(implicit val storage: Storage) {
      def load(key: String) = storage.read(key)
      def save(key: String, value: String) = storage.write(key, value)
    }
  
    class CachedPersistence(cache: CacheDriver)(implicit override val storage: Storage) extends Persistence {
      override def load(key: String) = {
        cache.get(key) match {
          case Some(value) => value
          case None => {
            val value = super.load(key)
            cache.put(key, value)
            value
          }
        }
      }
      override def save(key: String, value: String) {
        cache.put(key, value)
        super.save(key, value)
      }
    }
  }
  
  object BridgeDemo {
    def demoFactory() {
      import factory._
      val disk = new FileStorage
      val db = new DatabaseStorage
      Storage.storage = disk
      val persistDisk = new Persistence
      Storage.storage = db
      val persistDB = new Persistence
      Storage.storage = disk
      val cachedPersistDisk = new CachedPersistence(new SingleCache)
      
      println("""
      |Bridge Pattern Demo for classes approach with Abstract Factory
      |Created three difference persistence things:
      |disk: Persistence with Storage factory set to FileStorage
      |database: Persistence with Storage factory set to DatabaseStorage
      |cached disk: CachedPersistence using SingleCache with Storage factory set to FileStorage

      |For each in turn, perform two operations:
      |1. save("foo", "bar")
      |2. load("foo")
      """.stripMargin)
      
      println("disk")
      persistDisk.save("foo", "bar")
      persistDisk.load("foo")
      
      println("database")
      persistDB.save("foo", "bar")
      persistDB.load("foo")
      
      println("cached disk")
      cachedPersistDisk.save("foo", "bar")
      cachedPersistDisk.load("foo")
    }
    
    def demoImplicitFileStorage() {
      import implicits._
      implicit val disk = new FileStorage
      val persist = new Persistence
      val cachedPersist = new CachedPersistence(new SingleCache)
      
      println("""
      |Bridge Pattern Demo for classes approach with implicit storage objects
      |Created two difference persistence things:
      |disk: Persistence with implicit FileStorage
      |cached disk: CachedPersistence using SingleCache with implicit FileStorage

      |For each in turn, perform two operations:
      |1. save("foo", "bar")
      |2. load("foo")
      """.stripMargin)
      
      println("disk")
      persist.save("foo", "bar")
      persist.load("foo")
      
      println("cached disk")
      cachedPersist.save("foo", "bar")
      cachedPersist.load("foo")
    }
    
    def demoImplicitDBStorage() {
      import implicits._
      implicit val db = new DatabaseStorage
      val persist = new Persistence
      val cachedPersist = new CachedPersistence(new SingleCache)
      
      println("""
      |Bridge Pattern Demo for classes approach with implicit storage objects
      |Created two difference persistence things:
      |database: Persistence with implicit DatabaseStorage
      |cached database: CachedPersistence using SingleCache with implicit DatabaseStorage

      |For each in turn, perform two operations:
      |1. save("foo", "bar")
      |2. load("foo")
      """.stripMargin)
      
      println("database")
      persist.save("foo", "bar")
      persist.load("foo")
      
      println("cached database")
      cachedPersist.save("foo", "bar")
      cachedPersist.load("foo")
    }
  }
}

package withtraits {
  trait Storage {
    protected def read(key: String): String
    protected def write(key: String, value: String): Unit
  }
  
  trait FileStorage extends Storage {
    protected def read(key: String) = {
      println("Reading " + key + " from file storage")
      "read_from_file_result"
    }
    protected def write(key: String, value: String) {
      println("Writing " + value + " for " + key + " to file storage")
    }
  }
  
  trait DatabaseStorage extends Storage {
    protected def read(key: String) = {
      println("Reading " + key + " from database")
      "read_from_database_result"
    }
    protected def write(key: String, value: String) {
      println("Writing " + value + " for " + key + " to database")
    }
  }
  
  trait Persistence { self: Storage =>
    def load(key: String) = read(key)
    def save(key: String, value: String) = write(key, value)
  }
  
  trait CacheDriver {
    protected def get(key: String): Option[String]
    protected def put(key: String, value: String): Unit
  }
  
  trait SingleCache extends CacheDriver {
    var cachedKey: String = _
    var cachedValue: String = _
    
    protected def get(key: String) = {
      if (key == cachedKey) {
        println("Found " + cachedValue + " for " + key + " in cache")
        Some(cachedValue) 
      } else None
    }
    protected def put(key: String, value: String) {
      println("Caching " + value + " for " + key)
      cachedKey = key
      cachedValue = value
    }
  }
  
  trait CachedPersistence extends Persistence { self: Storage with CacheDriver =>
    override def load(key: String) = {
      get(key) match {
        case Some(value) => value
        case None => {
          val value = super.load(key)
          put(key, value)
          value
        }
      }
    }
    override def save(key: String, value: String) {
      put(key, value)
      super.save(key, value)
    }
  }
  
  object BridgeDemo {
    def demo() {
      val disk = new Persistence with FileStorage
      val db = new Persistence with DatabaseStorage
      val cachedDisk = new CachedPersistence with FileStorage with SingleCache
      
      println("""
      |Bridge Pattern Demo for traits approach
      |Created three difference persistence things:
      |disk: Persistence mixed with FileStorage
      |database: Persistence mixed with DatabaseStorage
      |cached disk: CachedPersistence mixed with FileStorage mixed with SingleCache

      |For each in turn, perform two operations:
      |1. save("foo", "bar")
      |2. load("foo")
      """.stripMargin)
      
      println("disk")
      disk.save("foo", "bar")
      disk.load("foo")

      println("database")
      db.save("foo", "bar")
      db.load("foo")

      println("cached disk")
      cachedDisk.save("foo", "bar")
      cachedDisk.load("foo")
    }
  }
}
