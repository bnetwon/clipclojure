package mys;
import java.rmi.Remote;
import java.rmi.RemoteException;

public interface MyService extends Remote {

    String RMI_ID = MyService.class.getCanonicalName();
    int PORT = 6666;

    void setInt(int ret) throws RemoteException;

    void voidMethod() throws RemoteException;

    int intMethod() throws RemoteException;
}